import React from 'react';
import classNames from 'classnames';

export default React.createClass({

    render: function () {

        const classString = classNames(
            'fa fa-folder fa-2x btn btn-default',
            {'active': this.props.showChords}
        );

        return (
            <li>
                <button
                    className={classString}
                    onClick={this.props.onToggleShowChords}
                />
            </li>
        );
    }

});


