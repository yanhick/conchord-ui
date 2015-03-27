import React from 'react';
import classNames from 'classnames';

export default React.createClass({

    render: function () {

        const classString = classNames(
            `fa fa-${this.props.icon} fa-2x btn btn-default`,
            {'active': this.props.active}
        );

        return (
            <button
                className={classString}
                onClick={this.props.onClick}
            />
        );
    }

});


