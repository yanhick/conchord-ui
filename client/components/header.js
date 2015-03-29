import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <header>
                <button
                    className="fa fa-home fa-2x"
                    onClick={this.props.onGoHome}
                    value="home" />
                {this.props.children}
            </header>
        );
    }

});
