import React from 'react';

export default React.createClass({

    handleIncrement(e) {
        e.preventDefault();
        this.props.onIncrement();
    },

    handleDecrement(e) {
        e.preventDefault();
        this.props.onDecrement();
    },

    render: function () {
        return (
            <form>
                <button onClick={this.handleIncrement} value="+" />
                <button onClick={this.handleDecrement} value="-" />
            </form>
        );
    }

});
